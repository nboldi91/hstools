```
sudo -i -u postgres
psql -d repo
```


```
stack run hstools-exe
```

```
stack exec -- ghc -c -fno-code -fplugin Language.Haskell.HsTools.Plugin -fplugin-opt Language.Haskell.HsTools.Plugin:'postgresql://saver:saver@127.0.0.1:5432/repo' fixtures/SimpleTest.hs
```