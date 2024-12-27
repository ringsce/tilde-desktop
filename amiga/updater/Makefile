CC := gcc
CFLAGS := -Wall -O2
LDFLAGS := -lamiga -lintuition

SRC := main.c gui.c torrent.c
OBJ := $(SRC:.c=.o)
OUT := torrent_client

all: $(OUT)

$(OUT): $(OBJ)
	$(CC) $(OBJ) -o $(OUT) $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(OUT)
