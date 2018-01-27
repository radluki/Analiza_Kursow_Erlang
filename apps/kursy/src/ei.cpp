/* ei.c */

#include "erl_interface.h"
#include "ei.h"
#include "erl_comm.h"
#include <map>
#include <string>


typedef unsigned char byte;

int main() {

  std::map<std::string,std::string> dict;
  ETERM *tuplep, *intp;
  ETERM *fnp, *loginp, *passwdp;
  int res;
  byte buf[100];
  long allocated, freed;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    loginp = erl_element(2, tuplep);
    passwdp = erl_element(3,tuplep);
    std::string fun = std::string(ERL_ATOM_PTR(fnp));
    std::string login = std::string(ERL_ATOM_PTR(loginp));
    std::string password = std::string(ERL_ATOM_PTR(passwdp));

    if (fun == "register_user") {
    	if( dict.find(login) != dict.end() ) 
		intp = erl_mk_atom("user_already_registered");
	else{
		dict[login] = password;
		intp = erl_mk_atom("ok");
	}
    } else if ( fun == "check_password" ) {
        if( dict.find(login) == dict.end() )
		intp = erl_mk_atom("login_not_found");
	else if (dict[login] == password)
		intp = erl_mk_atom("ok");
	else
		intp = erl_mk_atom("wrong_password");
    }

    erl_encode(intp, buf);
    write_cmd(buf, erl_term_len(intp));

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(loginp);
    erl_free_term(passwdp);
    erl_free_term(intp);
  }
}
