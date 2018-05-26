evenL [] = []
evenL [a] = []
evenL [a,b] = [b]
evenL (x:y:xs) = y:evenL xs