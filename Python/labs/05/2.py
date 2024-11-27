import os

def caesar_cipher(s: str, k: int) -> str:
    return ''.join([chr((ord(c) - 97 + k) % 26 + 97) for c in s])

def caesar_cipher2(s: str, k: int) -> str:
    result = ''
    for x in s:
        if x == ' ':
            result += x
        elif x.isupper():
            result += chr((ord(x) - ord('A') + k) % 26 + ord('A'))
        elif x.islower():
            result += chr((ord(x) - ord('a') + k) % 26 + ord('a'))
        elif x.isdigit():
            result += chr((ord(x) - ord('0') + k) % 10 + ord('0'))
        else:
            result += x
    return result

class LAIKA:
    def __init__(self, directory: str, caesar_key: int) -> None:
        self.directory = directory
        self.caesar_key = caesar_key

    def encode(self, text: str, n: int) -> list[str]:
        text_length = len(text)
        start = text_length - 1 if text_length % 2 == 0 else text_length - 2

        indices = []

        for i in range(0 , text_length, 2):
            indices.append(i)

        for i in range(start, 0, -2):
            indices.append(i)

        end_string = ''.join(text[i] for i in indices)
        result = [end_string[i:i + n] for i in range(0, len(end_string), n)]
        return result
    
    def decode(self, encoded_parts: list[str]) -> str:
        encoded_string = ''.join(encoded_parts)

        encoded_string_length = len(encoded_string)

        start = encoded_string_length - 1 if encoded_string_length % 2 == 0 else encoded_string_length - 2

        indices = []

        for i in range(0 , encoded_string_length, 2):
            indices.append(i)

        for i in range(start, 0, -2):
            indices.append(i)


        decoded_string = [''] * encoded_string_length

        for idx, pos in enumerate(indices):
            decoded_string[pos] = encoded_string[idx]

        return ''.join(decoded_string)


    def encode_to_files(self, message: str, n: int) -> str:
        encoded_parts = self.encode(message, n)

        filenames = []

        for item in encoded_parts:
            filename = caesar_cipher(item, self.caesar_key)
            filenames.append(filename)

        for filename in filenames:
            path = os.path.join(self.directory, filename)
            if os.path.exists(path):
                raise FileExistsError(f"File {path} already exists.")
            
        for idx, (filename, item) in enumerate(zip(filenames, encoded_parts)):
            path = os.path.join(self.directory, filename)
            next_file_name = filenames[idx + 1] if idx + 1 < len(filenames) else ''

            with open(path, 'w') as f:
                f.write(next_file_name + '\n')
                f.write(item)

        return filenames[0]
    
    def decode_from_files(self, filename: str) -> str:
        encoded_parts = []

        current_filename = filename
        
        while current_filename:
            path = os.path.join(self.directory, current_filename)
            if not os.path.exists(path):
                raise FileNotFoundError(f"File {path} not found.")
            
            with open(path, 'r') as f:
                next_file_name = f.readline().strip()
                encoded_parts.append(f.readline())

            current_filename = next_file_name

        return self.decode(encoded_parts)
    
# Tests

# Preconditions
import os

root_dir = "task_2"
os.makedirs(root_dir)

l = LAIKA(root_dir, 3)

# encode
assert l.encode("abcdefg", 2) == ["ac", "eg", "fd", "b"]
assert l.encode("abcdefg", 3) == ["ace", "gfd", "b"]
assert l.encode("abcdefg", 5) == ["acegf", "db"]
assert l.encode("abcdefghijkl", 1) == ["a", "c", "e", "g", "i", "k", "l", "j", "h", "f", "d", "b"]
assert l.encode("abcdefghijkl", 2) == ["ac", "eg", "ik", "lj", "hf", "db"]
assert l.encode("abcdefghijkl", 3) == ["ace", "gik", "ljh", "fdb"]
assert l.encode("abcdefghijkl", 4) == ["aceg", "iklj", "hfdb"]
assert l.encode("abcdefghijkl", 4) == ["aceg", "iklj", "hfdb"]
assert l.encode("abcdefghijkl", 12) == ["acegikljhfdb"]
assert l.encode("abcdefghijkl", 24) == ["acegikljhfdb"]


# decode
assert l.decode(["ac", "eg", "fd", "b"]) == "abcdefg"
assert l.decode(l.encode("abcdefg", 3)) == "abcdefg"
assert l.decode(l.encode("abcdefg", 5)) == "abcdefg"
assert l.decode(l.encode("abcdefghijkl", 1)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 2)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 3)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 4)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 4)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 12)) == "abcdefghijkl"
assert l.decode(l.encode("abcdefghijkl", 24)) == "abcdefghijkl"


# encode_to_files
l1 = LAIKA(root_dir, 4)
assert l1.encode_to_files("abcdefghijkl", 3) == "egi"

assert sorted(os.listdir(root_dir)) == ["egi", "jhf", "kmo", "pnl"]

with open(os.path.join(root_dir, "egi")) as fp:
    next_file = fp.readline().strip()
    content = fp.readline().strip()

assert next_file == "kmo"
assert content == "ace"

with open(os.path.join(root_dir, "jhf")) as fp:
    next_file = fp.readline().strip()
    content = fp.readline().strip()

assert next_file == ""
assert content == "fdb"

with open(os.path.join(root_dir, "kmo")) as fp:
    next_file = fp.readline().strip()
    content = fp.readline().strip()

assert next_file == "pnl"
assert content == "gik"

with open(os.path.join(root_dir, "pnl")) as fp:
    next_file = fp.readline().strip()
    content = fp.readline().strip()

assert next_file == "jhf"
assert content == "ljh"


# decode_from_files
assert l1.decode_from_files("egi") == "abcdefghijkl"

# Exception

try:
    l1.encode_to_files("abcdefghijkl", 3)
except FileExistsError:
    assert True
except Exception:
    assert False


try:
    l1.decode_from_files("non-existing-file")
except FileNotFoundError:
    assert True
except Exception:
    assert False

print("✅ All OK! +2 points")