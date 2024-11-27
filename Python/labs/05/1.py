import os

class InvalidLineError(Exception):
    def __init__(self, line: str) -> None:
        self.line = line
        super().__init__(f"Invalid line: {line}")

class InvalidItemError(Exception):
    def __init__(self, item_name: str) -> None:
        self.item_name = item_name
        super().__init__(f"Invalid item: {item_name}")


class InvalidQuantityError(Exception):
    def __init__(self, quantity: str, item_name: str) -> None:
        self.quantity = quantity
        self.item_name = item_name
        super().__init__(f"Invalid quantity: {quantity} for item {item_name}")

class InvalidPriceError(Exception):
    def __init__(self, item_price: str, item_name: str) -> None:
        self.item_price = item_price
        self.item_name = item_name
        super().__init__(f"Invalid price: {item_price} for item {item_name}")

class ListFileError(Exception):
    def __init__(self, filepath) -> None:
        self.filepath = filepath
        super().__init__(f"Cannot read file: {filepath}")


def validate_list(filepath: str):
    total = 0

    if not os.path.exists(filepath):
        raise ListFileError(filepath)

    try:
        with open(filepath, 'r', encoding='utf-8') as file:
            lines = file.readlines()
    except OSError:
        raise ListFileError(filepath)
    
    for line in lines:
        if not line.startswith('-'):
            raise InvalidLineError(line)
        data = line[1:].split(':')

        if len(data) != 3:
            raise InvalidLineError(line)
    
        item_name, quantity , price = data

        if item_name == '' or item_name.isdigit():
            raise InvalidItemError(item_name)
        
        try:
            quantity_converted = int(quantity)
            if quantity_converted < 0:
                raise InvalidQuantityError(quantity, item_name)
        except ValueError:
            raise InvalidQuantityError(quantity, item_name)
        
        try:
            price_converted = float(price)
            if price_converted < 0:
                raise InvalidPriceError(price, item_name)
        except ValueError:
            raise InvalidPriceError(price, item_name)
    
        total += int(quantity) * float(price)

    return total
    

assert abs(validate_list(os.path.join("task_1", "list1.txt")) - 11.25) < 0.001

assert int(validate_list(os.path.join("task_1", "list2.txt"))) == 0, "Empty files should return 0"

try:
    validate_list(os.path.join("task_1", "list3.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidLineError:
    pass

try:
    validate_list(os.path.join("task_1", "list4.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidLineError:
    pass

try:
    validate_list(os.path.join("task_1", "list5.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidItemError:
    pass

try:
    validate_list(os.path.join("task_1", "list6.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidQuantityError:
    pass

try:
    validate_list(os.path.join("task_1", "list7.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidQuantityError:
    pass

try:
    validate_list(os.path.join("task_1", "list8.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidQuantityError:
    pass

try:
    validate_list(os.path.join("task_1", "list9.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidPriceError:
    pass

try:
    validate_list(os.path.join("task_1", "list10.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidPriceError:
    pass

try:
    validate_list(os.path.join("task_1", "list11.txt"))
    assert False, "Should raise InvalidLineError"
except InvalidLineError:
    pass

print("✅ All OK! +1 point")


    