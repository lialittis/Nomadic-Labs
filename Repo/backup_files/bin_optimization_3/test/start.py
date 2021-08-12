import sys
import subprocess

l = len(sys.argv)

if(l < 4):
    print('please give the following arguments in order:\n')
    print('num_terms, seed , filename, min_size(optional) , max_size(optional)\n')
    sys.exit()

list_command = sys.argv

print('arguments are ',str(list_command))

# command = ["tezos-snoop","alpha michelson generate","1", "terms of kind","code","in","--seed"]
command = ["tezos-snoop","alpha michelson generate","1", "terms of kind","data","in","--seed"]

test_command = ["tezos-snoop","--help"]


command.insert(7,list_command[2])
filename = "bef/"+list_command[3]
command.insert(6,filename)

if(l==5):
    command.insert(7,"--min-size "+list_command[4])
elif(l==6):
    command.insert(7,"--min-size "+list_command[4])
    command.insert(8,"--max-size "+list_command[5])

for i in range(int(list_command[1])):
    command = ["bef/"+list_command[3]+"_"+str(i) if s == filename else s for s in command ]
    filename = "bef/"+list_command[3]+"_"+str(i)
    str_command = " ".join(command)
    print('command is', str_command)
    print(subprocess.call(str_command,shell=True))




