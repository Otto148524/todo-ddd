import { useState, useEffect, useCallback } from 'react'
import { apiClient } from '../api/client'
import type { TaskDTO, TasksStatisticsDTO, TodoEventDTO } from '../types/api'

interface UseTodosReturn {
  tasks: TaskDTO[]
  statistics: TasksStatisticsDTO
  events: TodoEventDTO[]
  isConnected: boolean
  error: string | null
  addTask: (desc: string) => Promise<void>
  toggleTask: (id: string) => Promise<void>
  deleteTask: (id: string) => Promise<void>
  refresh: () => Promise<void>
}

export function useTodos(): UseTodosReturn {
  const [tasks, setTasks] = useState<TaskDTO[]>([])
  const [statistics, setStatistics] = useState<TasksStatisticsDTO>({
    totalCount: 0,
    activeCount: 0,
    isCompletedCount: 0,
  })
  const [events, setEvents] = useState<TodoEventDTO[]>([])
  const [isConnected, setIsConnected] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const refresh = useCallback(async () => {
    try {
      // Fetch tasks and statistics
      const tasksData = await apiClient.fetchTasks()
      setTasks(tasksData.tasks)
      setStatistics(tasksData.statistics)

      // Fetch events
      const eventsData = await apiClient.fetchEvents()
      setEvents(eventsData)

      setIsConnected(true)
      setError(null)
    } catch (err) {
      setIsConnected(false)
      setError(err instanceof Error ? err.message : 'Unknown error')
      throw err
    }
  }, [])

  const addTask = useCallback(
    async (desc: string) => {
      try {
        await apiClient.initiateTask(desc)
        await refresh()
      } catch (err) {
        setError('タスクの追加に失敗しました')
        throw err
      }
    },
    [refresh]
  )

  const toggleTask = useCallback(
    async (id: string) => {
      try {
        await apiClient.toggleTask(id)
        await refresh()
      } catch (err) {
        setError('タスクの更新に失敗しました')
        throw err
      }
    },
    [refresh]
  )

  const deleteTask = useCallback(
    async (id: string) => {
      try {
        await apiClient.deleteTask(id)
        await refresh()
      } catch (err) {
        setError('タスクの削除に失敗しました')
        throw err
      }
    },
    [refresh]
  )

  // Initial fetch
  useEffect(() => {
    refresh().catch((err) => {
      console.error('Initial fetch failed:', err)
    })
  }, [refresh])

  // Polling every 2 seconds
  useEffect(() => {
    const interval = setInterval(() => {
      refresh().catch((err) => {
        console.error('Polling error:', err)
      })
    }, 2000)

    return () => clearInterval(interval)
  }, [refresh])

  return {
    tasks,
    statistics,
    events,
    isConnected,
    error,
    addTask,
    toggleTask,
    deleteTask,
    refresh,
  }
}