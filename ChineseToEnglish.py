'''


Converts chinese names into english equivalent using an external package

def addtwo(a,b):
    return (a+b)

def addFixedValue(a):
  y = 5
  return y +a
  
print addtwo(1,2)
print addFixedValue(1)
def printme( str ):
   "This prints a passed string into this function"
   print (str)
   return
printme("this is test")

def addtwonum(a,b):
    print(a)
    print(b)
    return

addtwonum(3,4)
x=addtwo(4,6)
print(x)
'''
'''
import pinyin
x=pinyin.get(u'你好')
print(x)
'''
import pinyin
#x=file.readline()
'''
x="刘 敏 华"
y=pinyin.get(x)
print(y)
print (" ".join(x))
t = s.split(" ")
if len(t) > 1:
  print "several tokens"

'''

#myString = "Position of a character"
#res=myString.index(' ')
#print(res)

'''
def findOccurences(s, ch):
    temp=[i for i, letter in enumerate(s) if letter == ch]
    return (temp)


res=findOccurences("test string is this", " ")
print(res)

print(res[1])
print(res[2])
print(res[0])
'''
#newstr = oldstr[:4] + oldst[5:]

'''
a="this is a string"
b=a.split(" ")
print(b)

c=len(b)
print(c)
d=b[1]+b[2]
print(d)

for x in range(0, 3):
    print "We're on time %d" % (x)
    
a="this is a string"
str=a.split(" ")
print(str)

a="this is a string"
str=a.split(" ")
res=fixSpaces(str)
res=res.title()
print(res)
'''

def fixSpaces(str):
    l=len(str)
   # print(l)
    temp=""
    if l>2:
        for i in range(1,l):
            temp=temp+str[i]

    temp=str[0]+" "+temp
    #print(temp)
    return (temp)


fileWrite = open("d:/newfile.txt", mode='w', encoding="utf8")   # open file for writing the output
file = open('d:/cn.txt', mode='r', encoding="utf8")            # open file for reading input

for line in file:                                    # loop to read all the line one by one
    x1=line                                           # read first line
    x2=" ".join(x1)                                    # insert spaces after each chinese character
    x3 = x2.split(" ")                                   # split into several strings based on spaces 
    if len(x3) > 1:                                        # if more than two spaces, remove extra spaces
      x4=fixSpaces(x3)   
    x5=pinyin.get(x4)
    x6=x5.title()
    fileWrite.write(x6)
    #fileWrite.write("\n")
    #print(y)
    
file.close()
#fileWrite.write("And here is another line\n")
fileWrite.close()

print("Execution Finished")
