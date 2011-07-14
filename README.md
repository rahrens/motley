Motley Erlang Utilities
=======================

Motley is a place for Erlang utilities to live.  My goals are roughly as follows:

* Have a place to keep shared dependencies which I am using in one or more of my other personal projects.

* Put some code out there and possibly gain some feedback as to how it could be improved.

* Provide something which perhaps could be useful to someone.

As such, Motley is not an application, nor is it intended to become one.  My 
hope is to keep dependencies between different Motley modules to a relative
minimum in the hopes that individual modules could be contributed to a project
without requiring the whole thing to be set up as a dependency.  But I will wait
to solve that problem until I have enough code here that it is an actual problem
and not a theoretical one.

Components
----------

Currently, Motley contains the following components:

* Motley_lazy : A simple lazy sequences library for Erlang.  Includes a bunch of helper utilities and supports MFA, closure and {Fun, Args} based sequences for all your lazy needs.


Building Motley
---------------

Motley has one dependency: [Basho Rebar](https://github.com/basho/rebar).  You will
need to install it into the project root directory, following the instructions on
that page in order to build Motley.
