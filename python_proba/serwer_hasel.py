#!/usr/bin/env python3

import os
import sys


def read_cmd():
	buf = read_exact(2)
	if len(buf) != 2:
		return -1
	L = int(buf)
	return read_exact(L)

def write_cmd(cmd):
	li = chr( (len(cmd) >> 8) & 0xff ).encode('utf-8')
	assert len(li) == 1
	write_exact(li)
	li = chr( len(cmd) & 0xff ).encode('utf-8')
	write_exact(li)
	return write_exact(cmd)


def read_exact(L):
	Data = b''
	got = 0
	while got<L:
		Data += os.read(0,L-got)
		if got == len(Data):
			break;
		got = len(Data)
	return Data

def write_exact(B):
	wrote = 0
	while wrote<len(B):
		i = os.write(1,B[wrote:])
		if i == 0:
			break;
		wrote+=i
	return wrote

if __name__=="__main__":
	D = dict()
	while True:
		buf = read_cmd()
		if len(buf) == 0:
			break
		fn = int(buf[0])
		uid = int(buf[1])
		haslo = str(buf[2:])

		if fn==1:
			if uid not in D.keys():
				D[uid] = haslo
				write_cmd(b'ok')
			else:
				write_cmd(b'user_already_redistered')
		else:
			if uid in D.keys():
				if haslo == D[uid]:
					write_cmd(b'ok')
				else:
					write_cmd(b'wrong_password')
			else:
				write_cmd(b'user_not_found')
			
