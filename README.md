<p align="center"><img src="./docs/logo.png"></p>

# Schedra
scripting language for linux kernel scheduler.

a prototype for eBPF summit 2025 hackathon

## Requirements
- **Linux Kernel >= 6.12**
  With:
  - `CONFIG_SCHED_CLASS_EXT=y`
  - `CONFIG_DEBUG_INFO_BTF=y` (or `BTF_DEBUG_INFO`)
- **Clang/LLVM >= 17**
- `libelf-dev`, `libbpf-dev` and `libseccomp-dev` installed

## Example
```
on dequeue(task) {
  if pid == 9924 {
    task.dispatch = 0;
  }
}
```
see the [reference guide](./docs/reference_guide.md) for more details.

## Resources
- [building a scripting language for kernel scheduler (my livestream)](https://www.youtube.com/watch?v=u3aTpKRcXQQ)

## Roadmap
- [ ] JIT compilation
- [ ] expression optimizer
- [ ] hot-reload scheduling logic
- [ ] migrate to LLVM backend for advanced optimizations
- [ ] more hooks (E.g. metrics/topology)
