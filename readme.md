# Aquarium Simulation #


### Compilation: ###
1. Go to src/
2. Run erl
3. cover:compile_directory().
4. Starting server: 
    - default port: aquarium:start().
    - aquarium:start(PortNumber).
5. Starting client:
    - default host and port: aquariumClient:start().
    - default port: aquariumClient:start(Host).
    - aquariumClient:start(Host, PortNumber).