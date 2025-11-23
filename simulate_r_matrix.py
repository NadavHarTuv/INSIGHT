import numpy as np

phi_ordering = [4, 3, 5, 1, 2]
phi = [0.316, 0.269, 0.254, 0.08, 0.08]
significance = np.array([
    [False, False, True, True, True],
    [False, False, True, True, True],
    [True, True, False, True, False],
    [True, True, True, False, True],
    [True, True, False, True, False],
])

num = len(phi_ordering)
rows = 2 + num
cols = 1 + num
temp = [["" for _ in range(cols)] for _ in range(rows)]

for i in range(num):
    temp[2 + i][0] = f"Phi[{phi_ordering[i]}]"

for j in range(num):
    temp[0][1 + j] = f"[{phi_ordering[j]}]|{phi[j]:.2f}"
    temp[1][1 + j] = "=" * (len(temp[0][1 + j]) + 1)

for j in range(num):
    for i in range(num):
        if significance[i, j] and phi_ordering[i] < phi_ordering[j]:
            temp[1 + phi_ordering[i]][phi_ordering[j]] = "X"

print("R-like matrix:")
for row in temp:
    print(row)

print("\nRows used for display (without headers column):")
for r in range(2, rows):
    print(temp[r][1:])

