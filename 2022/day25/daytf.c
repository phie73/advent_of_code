#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>

static inline bool is_snafu(char digit)
{
    return (digit == '=' || digit == '-' || digit == '0' || digit == '1' || digit == '2');
}

static int64_t snafu_to_decimal(char *snafu)
{
    const int64_t snafu_len = strlen(snafu);

    int64_t decimal = 0;           
    int64_t snafu_magnitude = 1;    

    for (int64_t i = snafu_len - 1; i >= 0; i--)
    {
        char snigit = snafu[i];
        
        int64_t value;
        switch (snigit)
        {
            case '=':
                value = -2;
                break;
            case '-':
                value = -1;
                break;
            case '0':
                value = 0;
                break;
            case '1':
                value = 1;
                break;
            case '2':
                value = 2;
                break;
            case '\n':  
                if (i == snafu_len-1) continue;
            default:
                fprintf(stderr, "Error: Invalid SNAFU number '%s'\n", snafu);
                abort();
                break;
        }
        decimal += value * snafu_magnitude;

        snafu_magnitude *= 5;
    }
    return decimal;
}

static size_t decimal_to_snafu(int64_t decimal, char *snafu, size_t buffer_size)
{
    static const char SNAFU_BASE[] = {'=', '-', '0', '1', '2'};
    static const char *SNAFU_TABLE = &SNAFU_BASE[0] + 2;
    
    if (decimal < 0)
    {
        fprintf(stderr, "Error: The decimal number cannot be negative.");
        abort();
    }
    
    int64_t temp_val = decimal;
    size_t count = 0;  
    
    do
    {
        temp_val /= 5;
        count++;
    }
    while (temp_val > 1);

    if (snafu == NULL) return count;
    
    if (buffer_size < count + 1)
    {
        fprintf(stderr, "Error: Insufficient buffer size of '%lu' for decimal '%ld'\n", count, decimal);
        abort();
    }
    int64_t pos = count+1;
    snafu[pos--] = '\0';    

    while (decimal > 0)
    {

        int64_t value = decimal % 5;
        if (value >= 3) value -= 5;
        
        char snigit = SNAFU_TABLE[value];
        assert(pos >= 0);
        snafu[pos--] = snigit;

        decimal -= value;
        decimal /= 5;
    }
    
    if (pos == 0)
    {
        memmove(&snafu[0], &snafu[1], count);
        count--;
    }
    
    return count;
}

int main(int argc, char **argv)
{
    FILE *input = fopen("input.txt", "rt");
    char line[32];

    int64_t total = 0;

    while (fgets(line, sizeof(line), input))
    {
        total += snafu_to_decimal(line);
    }

    fclose(input);

    char solution[128];
    decimal_to_snafu(total, solution, sizeof(solution));

    printf("Part 1: %s\n", solution);    
    return 0;
}