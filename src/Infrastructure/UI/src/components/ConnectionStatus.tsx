interface ConnectionStatusProps {
  isConnected: boolean
}

export function ConnectionStatus({ isConnected }: ConnectionStatusProps) {
  return (
    <div
      className={`px-4 py-3 rounded-lg text-center font-semibold mb-6 ${
        isConnected
          ? 'bg-green-100 text-green-800'
          : 'bg-red-100 text-red-800'
      }`}
    >
      {isConnected ? '✓ Connected to Haskell Server' : '✗ Disconnected from Server'}
    </div>
  )
}