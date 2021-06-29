// Prints the first 100 prime numbers

func is_prime(n: int) -> bool
{
    local i: int = 2;
    local d: int;

    while i < n
    {
        math d = n % i;
        if d == 0 { return false; }

        inc i;
    }

    return true;
}

func main() -> int
{
    local i: int = 0;

    while i < 100
    {
        if is_prime(i) == true 
        {
            call print_int(i);
            call print_char(' ');
        }

        inc i;
    }
}
