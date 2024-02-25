
## FOR LOOP
for(i in 1:10)
{
  print(i)
}

for (i in 1:5) {
  print(i + 1)
}

#FACTORIALS
fact <- 1
for(i in 1:10)
{
  fact <- fact * i
  print(paste(i,'factorial is',fact))
}

#else if statement
for(i in 1:10)
{
  if(i < 4)
  {
    print(paste(i, 'is less than 4'))
  } else if(i < 7) {
    print(paste(i, 'is less than 7 but not 4'))
  } else {
    print(paste(i, 'is neither'))
  }
}
## WHILE LOOP
#while(logical_condition){ expression }
#FACTORIALS
fact <- 1
i <- 1
while(i <= 10)
{
  fact = fact * i
  print(paste(i,'factorial is ',fact))
  i <- i + 1
}




