// Type definitions matching Haskell DTOs

export interface TaskDTO {
  taskId: string
  desc: string
  isCompleted: boolean
}

export interface TasksStatisticsDTO {
  totalCount: number
  activeCount: number
  isCompletedCount: number
}

export interface TasksResponse {
  tasks: TaskDTO[]
  statistics: TasksStatisticsDTO
}

export interface TodoEventDTO {
  id: string
  type: string
  desc: string | null
  timestamp: string
}

export interface InitiateTaskRequest {
  requestText: string
}

export interface InitiateTaskResponse {
  createId: string
}

export interface ToggleRequest {
  toggleId: string
}

export interface DeleteRequest {
  deleteId: string
}