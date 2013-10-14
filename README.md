Xen Test VM.

Build the server with:

cd xen-testvm
opam switch mirage-xen
make
make install
cd server
mirari configure --xen
mirari build --xen
sudo `which mirari` run --xen

Build the client with:
cd xen-testvm
opam switch mirage-unix
make
./setup.bin -configure --enable-cli
make



