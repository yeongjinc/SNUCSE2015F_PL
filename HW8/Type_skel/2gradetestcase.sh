#!/bin/bash

echo "====================test ===================="
./run ./examples/test0.m
echo "((int , int) , (bool , bool))"

echo "====================test 1===================="
./run ./examples/test1.m
echo "(int , bool)"

echo "====================test 2===================="
./run ./examples/test2.m
echo "Type Checking Failed"

echo "====================test 3===================="
./run ./examples/test3.m
echo "int"

echo "====================test 4===================="
./run ./examples/test4.m
echo "Type Checking Failed"

echo "====================test 5===================="
./run ./examples/test5.m
echo "(string , bool)"

echo "====================test 6===================="
./run ./examples/test6.m
echo "Type Checking Failed"

echo "====================test 7===================="
./run ./examples/test7.m
echo "((int , int) , (bool , bool))"

echo "====================test 8===================="
./run ./examples/test8.m
echo "Type Checking Failed"

echo "====================test 9===================="
./run ./examples/test9.m
echo "int"

echo "====================test 10===================="
./run ./examples/test10.m
echo "Type Checking Failed"

echo "====================test 11===================="
./run ./examples/test11.m
echo "(int , string)"

echo "====================test 12===================="
./run ./examples/test12.m
echo "Type Checking Failed"

echo "====================test 13===================="
./run ./examples/test13.m
echo "(int , bool)"

echo "====================test 14===================="
./run ./examples/test14.m
echo "Type Checking Failed"

echo "====================test 15===================="
./run ./examples/test15.m
echo "(bool , (bool , bool))"

echo "====================test 16===================="
./run ./examples/test16.m
echo "Type Checking Failed"

echo "====================test 17===================="
./run ./examples/test17.m
echo "((int , string) , (bool , loc (int)))" 

echo "====================test 18===================="
./run ./examples/test18.m
echo "Type Checking Failed"

echo "====================test 19===================="
./run ./examples/test19.m
echo "(bool , string)"

echo "====================test 20===================="
./run ./examples/test20.m
echo "Type Checking Failed"

echo "====================test 21===================="
./run ./examples/test21.m
echo "(bool , bool)"

echo "====================test 22===================="
./run ./examples/test22.m
echo "Type Checking Failed"

echo "====================test 23===================="
./run ./examples/test23.m
echo "string"

echo "====================test 24===================="
./run ./examples/test24.m
echo "Type Checking Failed"

echo "====================test 25===================="
./run ./examples/test25.m
echo "Type Checking Failed"

echo "====================test 26===================="
./run ./examples/test26.m
echo "bool"

echo "====================test 27===================="
./run ./examples/test27.m
echo "(bool , bool)"

echo "====================test 28===================="
./run ./examples/test28.m
echo "Type Checking Failed"

echo "====================test 29===================="
./run ./examples/test29.m
echo "bool"

echo "====================test 30===================="
./run ./examples/test30.m
echo "(bool , string)"

echo "====================test 31===================="
./run ./examples/test31.m
echo "Type Checking Failed"
