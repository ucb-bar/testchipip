ifndef RISCV
$(error RISCV is unset)
else
$(info Running with RISCV=$(RISCV))
endif

PREFIX ?= $RISCV/

default: libspikedevices.so

libspikedevices.so: devices.cc
	g++ -L $(RISCV)/lib -Wl,-rpath,$(RISCV)/lib -shared -o $@ -std=c++17 -I $(RISCV)/include -fPIC $^

.PHONY: install
install: libspikedevices.so
	cp libspikedevices.so $(RISCV)/lib

clean:
	rm -rf *.o *.so
