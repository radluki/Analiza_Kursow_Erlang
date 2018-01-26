1) compile extprog with:
	gcc -o extprg complex.c erl_comm.c port.c
or with
	./compile

2) run erl interpreter
	1> complex1:start("./extprg").
	<0.63.0>
	2> complex1:foo(5).
	6
	3> complex1:bar(5).
	10

