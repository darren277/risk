# Compiler and flags
FC = gfortran
FLAGS = -Wall -Wextra -O2

# Project files
SRC = ./src/loan_types.f90 ./src/db.f90 ./src/socket_helpers.f90 ./src/server_socket.f90 ./src/http_server.f90 ./src/json_wrapper.f90 ./src/risk_calc.f90 ./src/handlers.f90 ./src/router.f90 ./src/main.f90
OBJ = $(SRC:.f90=.o)

# Output binary
TARGET = risk.bin

.PHONY: all clean

# Build rules
all: $(TARGET)

$(TARGET): $(OBJ)
	$(FC) $(FLAGS) -o $(TARGET) $(OBJ)

%.o: %.f90
	$(FC) $(FLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)

run:
	PORT=8085 ./risk.bin
