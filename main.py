import sys

def add_token(token: str):

    with open('src/GoLexer.x') as fl:
        lines = fl.readlines()

    first_idx = -1
    second_idx = -1
    third_idx = -1

    for index, line in enumerate(lines):
        if 'last keywords first part' in line:
            first_idx = index
        if 'last keywords second part' in line:
            second_idx = index
        if 'last keywords third part' in line:
            third_idx = index

    if first_idx > 0 and second_idx > 0 and third_idx > 0:

        with open('src/GoLexer.x', 'w') as fl:
            for index, line in enumerate(lines):
                if index == first_idx:
                    fl.write(f'@{token} = {token}\n')
                    fl.write('-- last keywords first part\n')
                elif index == second_idx:
                    fl.write(f'@{token}' + ' {' + f'lex\' AlexRawToken_{token}' + '}\n')
                    fl.write('-- last keywords second part\n')
                elif index == third_idx:
                    fl.write(f'     | AlexRawToken_{token}\n')
                    fl.write( '     -- last keywords third part\n')
                else:
                    fl.write(line)

    with open('src/GoParser.y') as fl:
        lines = fl.readlines()

    first_idx = -1

    for index, line in enumerate(lines):
        if 'last keywords first part' in line:
            first_idx = index

    if first_idx > 0:

        with open('src/GoParser.y', 'w') as fl:
            for index, line in enumerate(lines):
                if index == first_idx:
                    fl.write(f'\'{token}\'' + ' { AlexTokenTag ' + f'AlexRawToken_{token} _' + ' }\n')
                    fl.write('-- last keywords first part\n')
                else:
                    fl.write(line)

def main():
    add_token(sys.argv[1])

if __name__ == "__main__":
    main()