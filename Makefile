CXX = g++
CXXFLAGS = -std=c++17 -Wall -g -fsanitize=address
SRC = $(wildcard *.cpp)
OBJ = $(SRC:.cpp=.o)
EXEC = lox

all: $(EXEC)

$(EXEC): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $^

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXEC)

.PHONY: all clean
