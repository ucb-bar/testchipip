
default: uart_tsi

uart_tsi: testchip_uart_tsi.cc ../src/main/resources/testchipip/csrc/testchip_tsi.cc
	g++ -O3 -L $(RISCV)/lib -I ../src/main/resources/testchipip/csrc -I $(RISCV)/include -std=c++17 -o $@ $^ -lfesvr -lpthread

clean:
	rm -rf uart_tsi
