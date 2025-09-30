import { useTodos } from './hooks/useTodos'
import { ConnectionStatus } from './components/ConnectionStatus'
import { Statistics } from './components/Statistics'
import { TaskInput } from './components/TaskInput'
import { TaskList } from './components/TaskList'
import { EventHistory } from './components/EventHistory'

function App() {
  const { tasks, statistics, events, isConnected, error, addTask, toggleTask, deleteTask } =
    useTodos()

  return (
    <div className="min-h-screen bg-gradient-to-br from-indigo-600 to-purple-700 flex items-center justify-center p-5">
      <div className="bg-white rounded-3xl shadow-2xl w-full max-w-7xl flex overflow-hidden">
        {/* Main Panel */}
        <div className="flex-1 p-10">
          <h1 className="text-4xl font-bold text-gray-800 mb-8">
            Todo App (Haskell Backend)
          </h1>

          <ConnectionStatus isConnected={isConnected} />

          <Statistics statistics={statistics} />

          <TaskInput onAdd={addTask} disabled={!isConnected} />

          {error && (
            <div className="bg-red-100 text-red-800 px-4 py-3 rounded-lg mb-5">
              {error}
            </div>
          )}

          <TaskList tasks={tasks} onToggle={toggleTask} onDelete={deleteTask} />
        </div>

        {/* Event Panel */}
        <div className="w-96 bg-gray-50 p-8 border-l border-gray-200 overflow-y-auto max-h-screen">
          <h2 className="text-xl font-bold text-gray-700 mb-5">Event History</h2>
          <EventHistory events={events} />
        </div>
      </div>
    </div>
  )
}

export default App