-module(motley_lazy_examples).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([collatz/2, lazy_fib/0, primes_sieve/0]).


%% Blagged from the sieve module in my main erlang directory.  This uses an efficient lazy version
%% of the Sieve of Eratosthenes.  It will piss all over most simpler primes counting algorithms
%% once you get any significant number of primes in it.
sieve(N, Sieve) ->
    case dict:find(N, Sieve) of
        {ok, ExistingIterators} ->
            NewIters = [{N + P, P} || P <- ExistingIterators], 
            Sieve2 = lists:foldl(fun ({X, P}, D) ->
                    dict:update(X, fun (L) -> [P|L] end, [P], D)
                end,
                dict:erase(N, Sieve),
                NewIters),
            sieve(N + 1, Sieve2);
        error ->
            Sieve2 = dict:update(N * N, fun (L) -> [N|L] end, [N], Sieve),
			motley_lazy:yield_result(N, fun sieve/2, [N + 1, Sieve2])
    end.


-spec primes_sieve() -> Primes::motley_lazy:lazy_seq().
primes_sieve() ->
	motley_lazy:from_fun_args(fun sieve/2, [2, dict:new()]).


%% Reasonable implementation of Fibonacci numbers:  Generate them by
%% counting up.
do_lazy_fib(Old, New) ->
	Current = Old + New,
	motley_lazy:yield_result(Current, fun do_lazy_fib/2, [New, Current]).


-spec lazy_fib() -> FibonnaciNumbers::motley_lazy:lazy_seq().
lazy_fib() ->
	motley_lazy:from_fun_args(fun do_lazy_fib/2, [0, 1]).


next_collatz(1) -> stop;
next_collatz(N) ->
	case N rem 2 of
		0 -> N div 2;
		1 -> (3 * N) + 1
	end.


do_collatz(N, Max) when N > Max ->
	motley_lazy:stop({maximum_exceeded, N, Max});
do_collatz(N, Max) ->			
	case next_collatz(N) of
		stop ->
			motley_lazy:stop_with_this(N, finished);
		NC ->
			motley_lazy:yield_result(N, fun do_collatz/2, [NC, Max])
	end.
		

-spec collatz(Start::integer(), Maximum::integer()) -> CollatzNumbers::motley_lazy:lazy_seq().
collatz(N, Max) ->
	motley_lazy:from_fun_args(fun do_collatz/2, [N, Max]).


-ifdef(TEST).

primes_sieve_test() ->
	Count = 1000,
	{ok, all, PrimeSeq, _Remaining} = motley_lazy:take(primes_sieve(), Count),
	{_FirstChunk, Last10} = lists:split(Count - 10, PrimeSeq), 
	?assertEqual([7841,7853,7867,7873,7877,7879,7883,7901,7907,7919], Last10).

-endif.
