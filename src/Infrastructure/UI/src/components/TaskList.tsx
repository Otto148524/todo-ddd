import type { TaskDTO } from '../types/api'
import { TaskItem } from './TaskItem'

interface TaskListProps {
  tasks: TaskDTO[]
  onToggle: (id: string) => Promise<void>
  onDelete: (id: string) => Promise<void>
}

export function TaskList({ tasks, onToggle, onDelete }: TaskListProps) {
  return (
    <ul className="space-y-0">
      {tasks.map((task) => (
        <TaskItem
          key={task.taskId}
          task={task}
          onToggle={onToggle}
          onDelete={onDelete}
        />
      ))}
    </ul>
  )
}