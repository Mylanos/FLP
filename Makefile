# the compiler: gcc for C program, define as g++ for C++
CC = ghc

# compiler flags:
#  -g     - this flag adds debugging information to the executable file
#  -Wall  - this flag is used to turn on most compiler warnings
CFLAGS  = -Wall
# The build target 
TARGET = flp22-fun
MAIN_PATH = src/Main.hs
all: $(TARGET)

$(TARGET): $(MAIN_PATH)
	$(CC) $(CFLAGS) -o $(TARGET) $(MAIN_PATH)

clean:
	$(RM) $(TARGET)