'''
Created on May 17, 2015


'''
with open('d:/engnames.txt',encoding="utf8") as f:
    lines = f.read().splitlines()

names=list()
for i in range (0,len(lines)):
    x=lines[i].split("\t")
    #if len(x)>1:
    if (x[1][1]=="_"):
        l=len(x[1])
        names.append(x[1][2:l])
                    
count=1
for i in range (0,(len(names)-1)):
    for j in range (i+1,len(names)):
        if (names[i]==names[j]):
          #  print(names[i])
           # print(names[j])
            names[j]=names[j]+ str(count)
            count=count+1
    count=1


for i in range (0,(len(names))):
    print(names[i])
    



#fileWrite = open("d:/namescorrected.txt", mode='w', encoding="utf8")   # open file for writing the output

#fileWrite.write(x6)


print("End of Execution")


