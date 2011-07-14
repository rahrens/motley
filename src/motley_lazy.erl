-module(motley_lazy). 

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Functions that return a lazy sequence.
-export([empty/0, empty/1, from_function/1, from_fun_args/2, from_mfa/3]).

%% Functions for manipulating lazy sequences.
-export([next/1, take/2, to_list/1, to_list/2]).

%% Internal functions for building lazy sequences.
-export([stop/1, stop_with_this/2, yield_result/2, yield_result/3, yield_result/4]).

%% Lazy sequence manipulation functions.
-export([map/2, combine/1, combine/2]).

%% Inspection functions.
-export([is_empty/1]).

-compile(export_all).


%% Typedefs
%% @type lazy_seq() = tuple().


%% @type next_el_result() = {ok, Next, lazy_seq()} | {stopped, Reason::term()}.
%% @spec next(lazy_seq()) -> next_el_result()
next({motley_lazy, {M, F, A}}) ->
	apply(M, F, A);
next({motley_lazy, {stopped, Reason}}) ->
	{stopped, Reason};
next({motley_lazy, {F, A}}) ->
	apply(F, A).


%% @spec is_empty(lazy_seq()) -> bool()
%% @doc Used for look-ahead to determine if the sequence is empty or not.  This is handled by
%% calculating the next member of the sequence.
%% @deprecated Avoid using this as it pays the cost up-front of generating the next member of 
%% the sequence -- something that will happen twice in a loop context.  The approved way to
%% check for emptiness is to use {@link next}:
%%
%% case next(LazySeq) of 
%%     {stopped, Reason} -> 
%%         ...
%%     {ok, Datum, LazySeq2} ->
%%         ...
%% end.
%%
is_empty(LazySeq) ->
	case next(LazySeq) of
		{stopped, _} ->
			true;
		_ ->
			false
	end.


do_combined([First|Rest]) ->
	do_combined_next(next(First), Rest);
do_combined([]) ->
	stop(exhausted).


do_combined_next({ok, Next, RestOfFirst}, Rest) ->
	yield_result(Next, fun do_combined/1, [[RestOfFirst|Rest]]);
do_combined_next({stopped, Reason}, []) ->
	{stopped, Reason};
do_combined_next({stopped, _}, MoreToCome) ->
	do_combined(MoreToCome).


%% @spec combine(list()) -> lazy_seq()
%% @doc Combine a list of lazy sequences into one.  Each sequence is iterated over in turn,
%% in the order in which they are passed in.
combine(Iterators) when is_list(Iterators) ->
	from_mfa(?MODULE, do_combined, [Iterators]).


%% @spec combine(lazy_seq(), lazy_seq()) -> lazy_seq()
%% @doc Combine two lazy sequences into one.
combine(FirstSeq={motley_lazy, _}, SecondSeq={motley_lazy, _}) ->
	combine([FirstSeq, SecondSeq]).


do_pushed(Term, Seq) ->
	{ok, Term, Seq}. 


%% @spec push(term(), lazy_seq()) -> lazy_seq()
%% @doc Push a term onto the front of the sequence.
push(Term, Seq={motley_lazy, _}) ->
	from_mfa(?MODULE, do_pushed, [Term, Seq]).


%% @spec empty() -> EmptySeq::lazy_seq()
empty() ->
	empty(empty).


%% @spec empty(Reason) -> EmptySeq::lazy_seq()
empty(Reason) ->
	{motley_lazy, stop(Reason)}.


%% @spec stop(Reason::term()) -> Stopped::next_el_result()
stop(Reason) ->
	{stopped, Reason}.


%% @spec stop_with_this(FinalResult::term(), StopReason::term()) -> LastResult::next_el_result()
stop_with_this(FinalResult, StopReason) ->
	{ok, FinalResult, empty(StopReason)}.


%% @spec take(lazy_seq(), integer()) -> {ok, stop_status(), Taken::list(), Remaining::lazy_seq()}
%% @type stop_status() = all | {early, Reason}.
take(PL2LS, N) when is_integer(N) andalso N >= 0 ->
	do_take(PL2LS, N, []).


do_take(PL2LS, 0, Acc) ->
	{ok, all, lists:reverse(Acc), PL2LS};
do_take(PL2LS, N, Acc) ->
	case next(PL2LS) of
		{ok, Next, LazySeq} ->
			do_take(LazySeq, N - 1, [Next|Acc]);
		{stopped, Reason} ->
			{ok, {early, Reason}, lists:reverse(Acc), empty(Reason)}
	end.


%% @spec from_fun_args(F::function(), Args::list()) -> lazy_seq()
from_fun_args(F, Args) when is_function(F) andalso is_list(Args) ->
	{motley_lazy, {F, Args}}.


%% @spec from_function(function()) -> lazy_seq()
%% @doc Convenient shorthand for from_fun_args(F, [])
from_function(F) when is_function(F) ->
	{motley_lazy, {F, []}}.


%% @spec from_mfa(Module::atom(), Function::atom(), Arguments::list()) -> lazy_seq()
%% @doc Constructor for lazy sequences using MFA pattern.  Note that this is probably the 
%% preferred pattern since only MFAs will take proper advantage of module reloading; funs will
%% not be properly refreshed.  Worth noting as a distinction, anyway.
from_mfa(Module, Function, Arguments) when is_atom(Module) andalso 
										   is_atom(Function) andalso 
										   is_list(Arguments) ->
	{motley_lazy, {Module, Function, Arguments}}.


%% @spec yield_result(Result::term(), Fun::function(), Args::list()) -> {ok, Result, lazy_seq()}
yield_result(Result, Fun, Args) ->
	{ok, Result, from_fun_args(Fun, Args)}.


%% @spec yield_result(Result::term(), Module::atom(), Function::atom(), Arguments::list()) -> {ok, Result, lazy_seq()}
yield_result(Result, Module, Function, Arguments) ->
	{ok, Result, from_mfa(Module, Function, Arguments)}.


%% @spec yield_result(Result::term(), Fun::function()) -> {ok, Result, lazy_seq}
yield_result(Result, Fun) ->
	{ok, Result, from_function(Fun)}.


do_map(Fun, LazySeq) ->
	case next(LazySeq) of
		{ok, Next, LazyTail} ->
			yield_result(Fun(Next), ?MODULE, do_map, [Fun, LazyTail]);
		{stopped, Reason} ->
			{stopped, Reason}
	end.

%% @spec map(Fun::function(), LazySeq::lazy_seq()) -> lazy_seq()
map(Fun, LazySeq) when is_function(Fun) ->
	from_mfa(?MODULE, do_map, [Fun, LazySeq]).


do_filter(Fun, LazySeq) ->
	case next(LazySeq) of
		{ok, Next, LazyTail} ->
			do_next_filter(Fun(Next), Next, Fun, LazyTail);
		{stopped, Reason} ->
			{stopped, Reason}
	end.


do_next_filter(true, Next, Fun, LazyTail) ->
	yield_result(Next, ?MODULE, do_filter, [Fun, LazyTail]);
do_next_filter(false, _Next, Fun, LazyTail) ->
	do_filter(Fun, LazyTail).


%% @spec filter(Filter::function(), LazySeq::lazy_seq()) -> lazy_seq()
%% @doc Filter LazySeq to only include members for whom Filter(Element) return
%% true.  Note that things will kerplode if Filter returns values which are not
%% of type bool().
filter(Fun, LazySeq) when is_function(Fun) ->
	from_mfa(?MODULE, do_filter, [Fun, LazySeq]).


%% @spec to_list(lazy_seq()) -> {list(), lazy_seq()}
to_list(LazySeq) ->
	to_list(LazySeq, 1000).


to_list(LazySeq, Max) ->
	{ok, _Status, List, Remainder} = take(LazySeq, Max),
	{List, Remainder}.


-ifdef(TEST).

int_counter(N) ->
	motley_lazy:yield_result(N, fun int_counter/1, [N + 1]).


first_ten_ints_test() ->
	IntIterator = motley_lazy:from_fun_args(fun int_counter/1, [0]),
	CountTo = 10,
	{ok, all, Seq, _Remaining} = motley_lazy:take(IntIterator, CountTo),
	?assertEqual(lists:seq(0, CountTo - 1), Seq).


map_test() ->
	IntIterator = motley_lazy:from_fun_args(fun int_counter/1, [0]),
	CountTo = 10,
	Square = fun (X) -> X * X end,
	SquaresIterator = motley_lazy:map(Square, IntIterator), 
	{ok, all, Seq, _Remaining} = motley_lazy:take(SquaresIterator, CountTo),
	?assertEqual(
	   [Square(Y) || Y <- lists:seq(0, CountTo - 1)],
	   Seq
	  ).


countdown_result() ->
	exhausted.


do_countdown(0) ->
	motley_lazy:stop(countdown_result());
do_countdown(N) when N > 0 ->
	motley_lazy:yield_result(N, fun do_countdown/1, [N - 1]);
do_countdown(N) ->
	motley_lazy:yield_result(N, fun do_countdown/1, [N + 1]).


countdown(N) ->	    
	motley_lazy:from_fun_args(fun do_countdown/1, [N]).


combine_test() ->
	Countdown5 = countdown(5),
	Countdown3 = countdown(3),
	?assertEqual(
	   {ok, {early, exhausted}, [5, 4, 3, 2, 1, 3, 2, 1], motley_lazy:empty(countdown_result())},
	   motley_lazy:take(motley_lazy:combine(Countdown5, Countdown3), 20)
	  ).


push_test() ->
	Countdown5 = countdown(5),
	?assertEqual(
	   {ok, {early, exhausted}, lists:seq(6, 1, -1), motley_lazy:empty(countdown_result())},
	   motley_lazy:take(motley_lazy:push(6, Countdown5), 20)
	  ).


filter_test() ->
	Countdown10 = countdown(10),
	Filter = fun (X) -> X rem 2 =:= 0 end,
	?assertMatch(
	   {[10, 8, 6, 4, 2], _},
	   motley_lazy:to_list(motley_lazy:filter(Filter, Countdown10))
	  ).
	   
-endif.
	   
					 
