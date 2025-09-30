import type {
  TasksResponse,
  TodoEventDTO,
  InitiateTaskRequest,
  InitiateTaskResponse,
  ToggleRequest,
  DeleteRequest,
} from '../types/api'

class TodoAPIClient {
  private baseURL: string

  constructor(baseURL: string = '/api') {
    this.baseURL = baseURL
  }

  async fetchTasks(): Promise<TasksResponse> {
    const response = await fetch(`${this.baseURL}/tasks`)
    if (!response.ok) throw new Error('Failed to fetch tasks')
    return await response.json()
  }

  async initiateTask(desc: string): Promise<InitiateTaskResponse> {
    const response = await fetch(`${this.baseURL}/tasks`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ requestText: desc } as InitiateTaskRequest),
    })
    if (!response.ok) throw new Error('Failed to initiate a task')
    return await response.json()
  }

  async toggleTask(id: string): Promise<void> {
    const response = await fetch(`${this.baseURL}/tasks/toggle`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ toggleId: id } as ToggleRequest),
    })
    if (!response.ok) throw new Error('Failed to toggle a task')
  }

  async deleteTask(id: string): Promise<void> {
    const response = await fetch(`${this.baseURL}/tasks/delete`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ deleteId: id } as DeleteRequest),
    })
    if (!response.ok) throw new Error('Failed to delete a task')
  }

  async fetchEvents(): Promise<TodoEventDTO[]> {
    const response = await fetch(`${this.baseURL}/events`)
    if (!response.ok) throw new Error('Failed to fetch events')
    return await response.json()
  }
}

export const apiClient = new TodoAPIClient()