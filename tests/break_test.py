arduino = Arduino(desktop=True)

@block
def test():
	i = 3
	while i > 0:
		x = 2
		while x > 0:
			x-=1
			print_i(x)
			if i == 2:
				break
			print_i(i)
		i-=1
	die()

arduino.send(test)