on dequeue(task) {
  if (task.pid == 1) {
    task.cpu = RL_CPU_ANY;
    task.slice = 12;
  }
}
