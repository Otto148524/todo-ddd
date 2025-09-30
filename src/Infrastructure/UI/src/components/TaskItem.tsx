import type { TaskDTO } from '../types/api'

interface TaskItemProps {
  task: TaskDTO
  onToggle: (id: string) => Promise<void>
  onDelete: (id: string) => Promise<void>
}

export function TaskItem({ task, onToggle, onDelete }: TaskItemProps) {
  return (
    <li
      className={`flex items-center p-4 bg-white border-2 border-gray-200 rounded-xl mb-3 transition-all hover:border-indigo-500 hover:shadow-md animate-slide-in ${
        task.isCompleted ? 'opacity-60' : ''
      }`}
    >
      <input
        type="checkbox"
        checked={task.isCompleted}
        onChange={() => onToggle(task.taskId)}
        className="w-6 h-6 mr-4 cursor-pointer"
      />
      <span
        className={`flex-1 text-base ${
          task.isCompleted ? 'line-through text-gray-400' : 'text-gray-800'
        }`}
      >
        {task.desc}
      </span>
      <button
        onClick={() => onDelete(task.taskId)}
        className="px-4 py-2 bg-red-500 text-white rounded-lg text-sm hover:bg-red-600 transition-colors"
      >
        削除
      </button>
    </li>
  )
}