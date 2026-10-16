# rpki-prover keeps up to a few GB of heap in the main process and each of
# its worker processes (see the --max-*-memory options); the "daemon" class
# allows 4G per process.
rpki_prover:\
	:datasize=8G:\
	:openfiles=4096:\
	:tc=daemon:
