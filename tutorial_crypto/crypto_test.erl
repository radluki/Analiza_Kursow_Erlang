-module(crypto_test).

-compile([export_all]).

test_encrypt_RSA(Msg) ->

	PrivateKey = public_key:generate_key({rsa,4096,65537}),
	MsgBin = term_to_binary(Msg),
	% {ok, PemBin} = file:read_file("key.pem"),
	% [RSAEntry] = public_key:pem_decode(PemBin),
 %    PrivateKey = public_key:pem_entry_decode(RSAEntry),
    {'RSAPrivateKey', 'two-prime', N, E, _D, _P, _Q, _E1, _E2, _C, _Other} = PrivateKey,
    PublicKey = {'RSAPublicKey',N,E},

	RsaEncrypted = public_key:encrypt_private(MsgBin, PrivateKey),
	MsgBin = public_key:decrypt_public(RsaEncrypted, PublicKey),

	RsaEncryptedPub = public_key:encrypt_public(MsgBin, PublicKey),
	MsgBin = public_key:decrypt_private(RsaEncryptedPub, PrivateKey),

	Msg = binary_to_term(MsgBin).

hash_SHA(Data) ->
	crypto:bytes_to_integer(crypto:hash(sha256,Data)).