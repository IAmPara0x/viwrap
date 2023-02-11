#!/usr/bin/python3


def main():
  # import termios as termios
  # import sys as sys

  # fd = sys.stdin.fileno()
  # old = termios.tcgetattr(fd)
  # old[3] = old[3] & ~termios.ICANON & ~termios.ECHO          # lflags
  # try:
  #     termios.tcsetattr(fd, termios.TCSANOW, old)
  # except Exception as e:
  #   print(e)
  #   return e

  while True:

    x = int(input("prompt> "))

    if x == 0:
      return 0

    print(f"result> {x**2=}")


if __name__ == "__main__":
    main()
