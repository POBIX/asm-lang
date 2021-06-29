// Prints the first 10 elements of the fibonacci sequence.

func print_fib(n: int) -> void
{
    local prev: int = 0;
    local curr: int = 1;
    local next: int;
    local i: int = 0;

    while i < n
    {
        call print_int(curr);
        call print_char(' ');

        math next = prev + curr;
        set prev = curr;
        set curr = next;

        inc i;
    }
}

func main() -> int
{
    call print_fib(10);
    return 0;
}
