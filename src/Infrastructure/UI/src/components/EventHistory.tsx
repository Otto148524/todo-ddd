import type { TodoEventDTO } from '../types/api'

interface EventHistoryProps {
  events: TodoEventDTO[]
}

export function EventHistory({ events }: EventHistoryProps) {
  // Show latest events first
  const reversedEvents = [...events].reverse()

  return (
    <div className="space-y-3">
      {reversedEvents.map((event, index) => {
        const timestamp = new Date(event.timestamp)
        const timeString = timestamp.toLocaleTimeString('ja-JP')

        return (
          <div
            key={index}
            className="p-3 bg-white rounded-lg text-sm animate-slide-in-right"
          >
            <div className="font-bold text-indigo-600 mb-1">{event.type}</div>
            {event.desc && (
              <div className="text-gray-600 font-mono text-xs bg-gray-100 p-2 rounded mt-2">
                {JSON.stringify(
                  {
                    id: event.id,
                    desc: event.desc,
                  },
                  null,
                  2
                )}
              </div>
            )}
            <div className="text-gray-400 text-xs mt-2">{timeString}</div>
          </div>
        )
      })}
    </div>
  )
}