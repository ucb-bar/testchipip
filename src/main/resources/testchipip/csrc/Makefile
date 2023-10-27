loadarch_main: loadarch_main.cc loadarch.cc loadarch.h
	g++ --std=c++17 \
		-I$(CONDA_PREFIX)/riscv-tools/include \
		-L$(CONDA_PREFIX)/riscv-tools/lib -Wl,-rpath,$(CONDA_PREFIX)/riscv-tools/lib \
		-lriscv -lfesvr \
		loadarch_main.cc loadarch.cc -o loadarch_main

clean:
	rm -f loadarch_main

.PHONY: clean
