```
sudo -i -u postgres
psql -d repo
```

```
delete from modules where filepath LIKE '%/fixtures/hstools-test/%';
```
