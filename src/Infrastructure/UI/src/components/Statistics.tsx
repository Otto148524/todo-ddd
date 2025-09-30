import type { TasksStatisticsDTO } from '../types/api'

interface StatisticsProps {
  statistics: TasksStatisticsDTO
}

export function Statistics({ statistics }: StatisticsProps) {
  return (
    <div className="flex gap-5 mb-6 p-5 bg-gray-50 rounded-xl">
      <div className="flex-1 text-center">
        <div className="text-2xl font-bold text-indigo-600">
          {statistics.totalCount}
        </div>
        <div className="text-sm text-gray-600 mt-1">Total Tasks</div>
      </div>
      <div className="flex-1 text-center">
        <div className="text-2xl font-bold text-indigo-600">
          {statistics.activeCount}
        </div>
        <div className="text-sm text-gray-600 mt-1">Active</div>
      </div>
      <div className="flex-1 text-center">
        <div className="text-2xl font-bold text-indigo-600">
          {statistics.isCompletedCount}
        </div>
        <div className="text-sm text-gray-600 mt-1">Completed</div>
      </div>
    </div>
  )
}