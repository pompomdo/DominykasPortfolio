# Python Core:

A batch of different Python exercises from resources online.

### 1. SORTED BINARY SEARCH

A more effective search relative on midpoint value.

    def count_numbers(sorted_list, item):
        begin_index = 0
        end_index = len(sorted_list) - 1
        value = 0
        while begin_index <= end_index:
            midpoint = begin_index + (end_index - begin_index) // 2
            midpoint_value = sorted_list[midpoint]
            if midpoint_value == item:
                return midpoint
            elif item < midpoint_value:
                end_index = midpoint - 1
            else:
                begin_index = midpoint +1
        for i in range(len(sorted_list)):
            if sorted_list[i] < item:
                value += 1
        return value

    sorted_list = [5, 4, 5, 7]

    print(count_numbers(sorted_list, 5)) # should print 2


### 1.B. DIFFERENT VERSION

    def count_numbers(sorted_list, item):
        for j in sorted_list:
            if item < j:
                break
            else: # binary sorted search for extra speed
                begin_index = 0
                end_index = len(sorted_list) - 1
                value = 0
                while begin_index <= end_index:
                    midpoint = begin_index + (end_index - begin_index) // 2
                    midpoint_value = sorted_list[midpoint]
                    if midpoint_value == item:
                        return midpoint
                    elif item < midpoint_value:
                        end_index = midpoint - 1
                    else:
                        begin_index = midpoint +1
                for i in range(len(sorted_list)):
                    if sorted_list[i] < item:
                        value += 1
                return value

    sorted_list = [5, 5, 5, 4]

    print(count_numbers(sorted_list, 4))



### 2. TRAIN COMPOSITION

Sequencing based on train composition idea. More effective way of retrieving items.

from collections import deque

class TrainComposition:

    def __init__(self):
        self.tc = deque('')
        print(self.tc)

    def attach_wagon_from_left(self, wagonId):
        self.wagonId = self.tc.appendleft(wagonId)
        print(self.tc)

    def attach_wagon_from_right(self, wagonId):
        self.wagonId = self.tc.append(wagonId)
        print(self.tc)

    def detach_wagon_from_left(self):
        return self.tc.popleft()

    def detach_wagon_from_right(self):
        return self.tc.pop()

    if __name__ == "__main__":
        train = TrainComposition()
        train.attach_wagon_from_left(7)
        train.attach_wagon_from_left(13)
        train.attach_wagon_from_left(23)
        train.attach_wagon_from_left(70)
        train.attach_wagon_from_left(70)
        train.attach_wagon_from_left(17)
        print(train.detach_wagon_from_right()) # should print 7 (first added)
        print(train.detach_wagon_from_left()) # should print 13 (second added)


### 3. GROUPING BY A COMMON FEATURE

    def group_by_owners(files):
        new_dictionary = {}
        for file, owner in files.items():
            new_dictionary[owner] = new_dictionary.get(owner, []) + [file]
        return new_dictionary

    files = {
        'Input.txt': 'Randy',
        'Code.py': 'Stan',
        'Output.txt': 'Randy'
    }

    print(group_by_owners(files))


### 4. QUADRATIC EQUATION

    import numpy as np

    def find_roots(a, b, c):
        sol1 = (-b + np.sqrt(b**2 - 4*a*c)) / (2*a)

##### I don't think there is a +- functionality available in Python yet
.
        sol2 = (-b - np.sqrt(b**2 - 4*a*c)) / (2*a)
        values_tuple = (sol1, sol2)
        return values_tuple

    print(find_roots(-4, 12, -9))


### 5. BINARY SEARCH TREE (without sorting)

    import collections

    Node = collections.namedtuple('Node', ['left', 'right', 'value'])
    print(Node)

    def contains(root, value):
        if root is None:
            return False
        if root.value == value:
            return True
        if root.value > value:
            return contains(root.left,value)
        if root.value < value:
            return contains(root.right,value)

    n1 = Node(value=1, left=None, right=None)
    n3 = Node(value=3, left=None, right=None)
    n2 = Node(value=2, left=n1, right=n3)

    print(contains(n2, 3))

### 6. IS PLAYLIST REPEATING?

    class Song:
        def __init__(self, name):
            self.name = name
            self.next = None

        def next_song(self, song):
            self.next = song

        def is_repeating_playlist(self):
            """
            :returns: (bool) True if the playlist is repeating, False if not.
            """
            playlist = {self}
            song = self.next
            while song:
                if song in playlist:
                    return True
                else:
                    playlist.add(song)
                    song = song.next
            return False

    first = Song("Hello")
    second = Song("Eye of the tiger")

    first.next_song(second)
    second.next_song(first)

    print(first.is_repeating_playlist())


### 7. FUNCTION FOR INDICATING WHICH NUMBERS SUM UP TO THE INDICATED NUMBER

    def find_two_sum(numbers, target_sum):
        """
        :param numbers: (list of ints) The list of numbers.
        :param target_sum: (int) The required target sum.
        :returns: (a tuple of 2 ints) The indices of the two elements whose sum is equal to target_sum
        """
        taken = {}
        for index, number in enumerate(numbers):
            diff = target_sum - number
            if diff in taken:
                return taken[diff], index
            taken[number] = index
        return None

    print(find_two_sum([0, 1, 4, 4, 3, 10], 10))


### 8. MERGING: REMAIN ONLY UNIQUE NAMES.

    def unique_names(names1, names2):
        return list(set().union(names1,names2))

    if __name__ == "__main__":
        names1 = ["Ava", "Emma", "Olivia"]
        names2 = ["Olivia", "Sophia", "Emma"]
        print(unique_names(names1, names2)) # should print Ava, Emma, Olivia, Sophia


### 9. Road Planner / minimum cost path

    def route_exists(from_row, from_column, to_row, to_column, map_matrix):
            if map_matrix[from_row][from_column] != True:
                return False            
            return True

    map_matrix = [
            [True, False, False],
            [True, True, False],
            [False, True, True]
        ];

    print(route_exists(0, 0, 2, 2, map_matrix))

#### 9.B.

    def route_exists(from_row, from_column, to_row, to_column, map_matrix):
        if (map_matrix[from_row][from_column] == True):
            for i in range(1, len(map_matrix)):
                if (map_matrix[i][0] != False):
                    map_matrix[i][0] = map_matrix[i-1][0]
                    print('rowz')

            for j in range(1, len(map_matrix[0])):
                if (map_matrix[0][j] != False):
                    map_matrix[0][j] = map_matrix[0][j-1]
                    print('colz')

            for i in range(1, len(map_matrix)):
                for j in range(1,len(map_matrix[0])):
                    if (map_matrix[to_row][to_column] != False):
                        return True
            return (map_matrix[len(map_matrix) - 1][len(map_matrix[0]) - 1] == 1)
        else:
            return False

    if __name__ == '__main__':
        map_matrix = [
            [True, True, False],
            [False, True, False],
            [False, True, True]
        ];

        print(route_exists(0, 0, 2, 2, map_matrix))


### 10. CATEGORIZING BETWEEN TWO CLASSES OF INGREDIENTS. ICE CREAM TOPPINGS AND FLAVOURS.

class IceCreamMachine:

    def __init__(self, ingredients, toppings):
        self.ingredients = ingredients
        self.toppings = toppings

    def scoops(self):
        res = []
        for i in self.ingredients:
            for j in self.toppings:
                res.append([i,j])
        return res


    machine = IceCreamMachine(["vanilla", "chocolate"], ["chocolate sauce"])
    print(machine.scoops()) #should print[['vanilla', 'chocolate sauce'], ['chocolate', 'chocolate sauce']]


### 11. PIPELINE: COMBINING MULTIPLE FUNCTIONS IN A SEQUENCE.

    def pipeline(*funcs):
        def helper(arg):
            for f in funcs:
                arg = f(arg)
            return arg
        return helper


    fun = pipeline(lambda x: x * 3, lambda x: x + 1, lambda x: x / 2)
    print(fun(4))

##### 12 > 13 > 6.5
