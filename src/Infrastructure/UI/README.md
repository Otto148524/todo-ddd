# Todo DDD - React UI

React frontend for Todo application implementing Event Sourcing and Hexagonal Architecture.

## Technology Stack

- **Vite** - Build tool
- **React 18** - UI library
- **TypeScript** - Type-safe development
- **TailwindCSS** - Utility-first CSS

## Features

- Task creation, completion, and deletion
- Real-time statistics display (Total/Active/Completed)
- Event Sourcing visualization (event history panel)
- Auto-polling every 2 seconds
- Connection status indicator
- Responsive design

## Setup

```bash
# Install dependencies
npm install

# Start development server
npm run dev
```

Development server runs at http://localhost:5173.

## Backend Server

Start the Haskell server in a separate terminal:

```bash
cd /work/todo-ddd
cabal run todo-ddd-exe
```

Backend API available at http://localhost:8080.

## API Endpoints

Vite development server proxies `/api` to `http://localhost:8080`.

- `GET /api/tasks` - Retrieve tasks and statistics
- `POST /api/tasks` - Create new task
- `POST /api/tasks/toggle` - Toggle task completion
- `POST /api/tasks/delete` - Delete task
- `GET /api/events` - Retrieve event history

## Project Structure

```
src/
├── types/
│   └── api.ts              # Type definitions matching Haskell DTOs
├── api/
│   └── client.ts           # REST API client
├── hooks/
│   └── useTodos.ts         # Task state management + polling
├── components/
│   ├── ConnectionStatus.tsx # Connection status display
│   ├── Statistics.tsx       # Statistics display
│   ├── TaskInput.tsx        # Task input form
│   ├── TaskList.tsx         # Task list
│   ├── TaskItem.tsx         # Individual task
│   └── EventHistory.tsx     # Event history
├── App.tsx                  # Root component
├── main.tsx                 # Entry point
└── index.css                # Global styles
```

## Build

```bash
# Production build
npm run build

# Preview build
npm run preview
```

Build output is in `dist/` directory.

## Development Notes

### Polling Logic

`useTodos.ts` implements auto-refresh every 2 seconds:

```typescript
useEffect(() => {
  const interval = setInterval(() => {
    refresh().catch((err) => {
      console.error('Polling error:', err)
    })
  }, 2000)

  return () => clearInterval(interval)
}, [refresh])
```

### Animations

Custom TailwindCSS animations:

- `animate-slide-in` - Task addition
- `animate-slide-in-right` - Event addition

### Type Safety

All API responses use TypeScript type definitions:

```typescript
interface TaskDTO {
  taskId: string
  desc: string
  isCompleted: boolean
}
```

## Future Enhancements

- WebSocket support for real-time notifications
- Dark mode
- Task filtering (All/Active/Completed)
- Task editing
- LocalStorage persistence