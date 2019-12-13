function rename_file (){
cp ./network_config_agent_0_layers_11_5_3_1.txt ./my_weights.txt
}
while :
do
    python3 run_car.py -s 800 --seed 3 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
    python3 run_car.py -s 800 --seed 13 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
    python3 run_car.py -s 800 --seed 23 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
    python3 run_car.py -s 800 --seed 4 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
    python3 run_car.py -s 800 --seed 14 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
    python3 run_car.py -s 800 --seed 24 -f network_config_agent_0_layers_11_5_3_1.txt
    rename_file
done