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
