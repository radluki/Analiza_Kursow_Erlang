Compilation
===========

It is necessary to compile serwer\_hasel written in cpp.
In order to do that:
	
	$ cd apps/kursy/src

Substitute ERL\_INT variable in Makefile to the path to erl\_interface on your system. Then compile serwer\_hasel

	$ make
	$ cd ../../..

Now you can compile the whole project with rebar3.


kursy
=====

An OTP application

Build
-----

    $ rebar3 compile

Eunit test
----------

    $ rebar3 eunit

Test manually with console
--------------------------

    $ rebar3 shell

    or

    $ rebar3 as test shell

While starting serwer on borg following applications were necessary:
inets,crypto,asn1,public\_key,ssl

Creating release
----------------

    $ rebar3 release

    or

    $ rebar3 tar
