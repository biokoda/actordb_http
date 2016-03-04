#!/bin/sh
echo "Trying 'exec' #1"
curl -vvv -u root:rootpass -d @test_exec_select.json -H "Content-Type: application/json" "http://localhost:33380/v1/q/exec"
echo "Trying 'exec' #2"
curl -vvv -u root:rootpass -d @test_exec_insert.json -H "Content-Type: application/json" "http://localhost:33380/v1/q/exec"
echo "Trying 'exec_single' #1"
curl -vvv -u root:rootpass -d @test_exec_single.json -H "Content-Type: application/json" "http://localhost:33380/v1/q/exec_single"
echo "Trying 'exec_single_param' #1"
curl -vvv -u root:rootpass -d @test_exec_single_param.json -H "Content-Type: application/json" "http://localhost:33380/v1/q/exec_single_param"
