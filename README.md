# crypto

```bash
stack test

# or with Docker
docker run --rm -it \
  -v $(pwd):/app -w /app \
  haskell stack test --allow-different-user --file-watch
```
