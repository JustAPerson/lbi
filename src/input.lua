return function(a, b)
	math.randomseed(0)
	
	print(a, b);
	print(a + b, a - b, a * b, a / b, a % b, a ^ b);
	print(a + 9, a - 6, a * 3, a / 2, a % 7, a ^ 2);
	print(-a, not b)
	print(#("test"))
	print("a" .. "b")
	
	if 2 + 2 == 4 then
		print(false or "Yay" and 3)
	end
	
	if math.random(0, 1) == 0 then
		print("huh")
	else
		print("hoh")
	end
	if 5 > 3 then
		print("magic")
	end
	if 3 <= 7 then
		print("double magic")
	end
	
	global_var = true
	print("global_var", global_var)
	global_var = nil
	print("global_var", global_var)

	return print("Magic", math.random(math.min(a, b), math.max(a,b+1)))
end