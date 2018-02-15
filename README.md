### How to run

- Run at once
```
scala -J-Xmx128g ./crellvm-tests-parallel/src/main/scala/main.scala -j 8 --opt-arg "-O2"  --opt-path <opt path> --vali-path <main.native path> -i <benchmark path>
```

- Validate specific pass only (e.g. run instcombine/gvn/sroa only)
```
scala -J-Xmx128g ./crellvm-tests-parallel/src/main/scala/main.scala -j 8 --opt-arg "-O2 --crellvm-passwhitelist 'instcombine'"  --opt-path <opt path> --vali-path <main.native path> -i <benchmark path>
```

- Validate specific optimizations only (e.g. validate 'load\_load', 'zext\_xor' optimizations only)
```
scala -J-Xmx128g ./crellvm-tests-parallel/src/main/scala/main.scala -j 8 --opt-arg "-O2 --crellvm-passwhitelist 'instcombine' --crellvm-whitelist 'zext_xor,load_load'"  --opt-path <opt path> --vali-path <main.native path> -i <benchmark path>
```
