-- Steps required to run the virtual machine:

1. Create the readable by the machine file required: 

$ base64 -d << ___EOF___ | zcat > pp.b
H4sICBBmBlwAA3BpbmctcG9uZy5iAD2OuwrCQBRE52YJjrFQu1ipYCEpNElpr629rQQN+IBEEKxsxW+0
FfErnESRhWXOmbvLZdfBa8G3G2gBI1FPdK2o8XLCsfBSl7ocJLveAs7iOHVIaE2bwq9Gf0ZPVrBCE2nS
9u74TgVC9baEn0paH5yE1gFHIYPQPETQ/wGHLLnTybjmQPnAY522PDHnWWlTu+Kf87rLuJct5Wbqqr7g
XKs8tf0boT2AD2rprlDrAAAA
___EOF___

2. Compile the ex5.cpp source code using g++ compiler, to create the virtual machine executable. 

$ g++ ex5.cpp -o ex5

3. Run the ex5 executable with the previously created file (step 1) as argument.
	$ ./ex5 test.b

