# Todo DDD Project

Haskell implementation of a Todo application using Domain-Driven Design (DDD) principles, Event Sourcing, and Hexagonal Architecture.

## Architecture Overview

The project follows a three-layer architecture with dependency inversion. Domain logic is isolated from infrastructure concerns through the Facade pattern.

## Module Dependencies

The following Mermaid diagram shows the dependency relationships between modules with the implemented Facade Pattern:

```mermaid
graph TB
    subgraph "Infrastructure Layer (Adapters)"
        InfraWebServer[Infrastructure.Web.Server]
        InfraWebTypes[Infrastructure.Web.Types]
        InfraMemory[Infrastructure.EventStore.InMemory]
        MainApp[Main]
    end

    subgraph "Application Layer (Use Cases & DTOs)"
        AppService[Application.TodoService]
        AppFacade[Application.DTO.Facade]
        AppDTO[Application.DTO.TodoDTO]
        AppEventPort[Application.Ports.EventStorePort]
        AppNotificationPort[Application.Ports.NotificationPort]
    end

    subgraph "Domain Layer (Pure Business Logic)"
        DomainAggregate[Domain.Todo.Aggregate]
        DomainEntity[Domain.Todo.Entity]
        DomainEvents[Domain.Todo.Events]
        DomainVO[Domain.Todo.ValueObject]
    end

    %% Domain internal dependencies
    DomainEntity --> DomainVO
    DomainEvents --> DomainVO
    DomainAggregate --> DomainEntity
    DomainAggregate --> DomainEvents
    DomainAggregate --> DomainVO

    %% Application layer - Aggregate Pattern implementation
    AppFacade --> DomainAggregate
    AppFacade --> AppDTO
    AppService --> AppFacade
    AppService --> AppDTO
    AppService --> AppEventPort
    AppService --> AppNotificationPort
    AppEventPort --> AppDTO
    AppNotificationPort --> AppDTO

    %% Infrastructure dependencies
    InfraMemory --> AppDTO
    InfraMemory --> AppEventPort
    InfraMemory --> AppNotificationPort
    InfraWebTypes --> AppDTO
    InfraWebServer --> AppFacade
    InfraWebServer --> AppDTO
    InfraWebServer --> AppService
    InfraWebServer --> InfraWebTypes
    InfraWebServer --> InfraMemory
    MainApp --> AppService
    MainApp --> AppFacade
    MainApp --> InfraMemory
    MainApp --> InfraWebServer

    %% Styling with facade pattern highlights
    classDef domain fill:#e1f5fe
    classDef application fill:#f3e5f5
    classDef infrastructure fill:#fff3e0
    classDef facade fill:#c8e6c9,stroke:#4caf50,stroke-width:3px
    classDef pureDto fill:#fff9c4,stroke:#ff9800,stroke-width:2px

    class DomainAggregate,DomainEntity,DomainEvents,DomainVO domain
    class AppService,AppFacade,AppDTO,AppEventPort,AppNotificationPort application
    class InfraWebServer,InfraWebTypes,InfraMemory,MainApp infrastructure
    class DomainAggregate,AppFacade facade
    class AppDTO pureDto
```

## Dependency Rules

The architecture implements Dependency Inversion Principle with Facade Pattern:

1. **Domain Layer**: Contains business logic without external dependencies
   - `Domain.Todo.ValueObject`: Value objects with validation
   - `Domain.Todo.Entity`: Domain entities
   - `Domain.Todo.Events`: Event definitions for event sourcing
   - `Domain.Todo.Aggregate`: Aggregate root providing domain boundary and `DTOConversionSupport`

2. **Application Layer**: Orchestrates use cases through aggregates
   - `Application.DTO.TodoDTO`: Data Transfer Objects without domain imports
   - `Application.DTO.Facade`: Bridge module importing only `Aggregate`
   - `Application.TodoService`: Service implementation using `DomainOperations`
   - `Application.Ports.*`: Infrastructure interface definitions

3. **Infrastructure Layer** (Orange): Technical implementations of ports
   - `Infrastructure.EventStore.InMemory`: STM-based event storage
   - `Infrastructure.Web.*`: Servant-based HTTP API implementation

4. **Main Application** (Pink): Dependency injection and startup

### Design Patterns

- Hexagonal Architecture: Separation of business logic from infrastructure
- Facade Pattern: `Domain.Todo.DomainFacade` provides domain interface
- Event Sourcing: State represented as event sequence
- CQRS: Command-Query Responsibility Segregation
- Dependency Injection: Port and Adapter pattern
- Anti-Corruption Layer: Domain isolation via facades

### Facade Pattern Implementation

The architecture employs dual facades for layer separation:

#### Domain.Todo.DomainFacade
- Input Types: `TodoCreationRequest`, `TodoUpdateRequest`
- Output Types: `DomainTodoView`, `DomainEventView`
- Internal Types: `TodoId`, `TodoText`, `Todo`, `TodoEvent`
- Functionality: `DTOConversionSupport` for type conversion
- Responsibilities: Event sourcing, validation, projection

#### Application.DTO.Facade
- Domain Import: `Domain.Todo.DomainFacade` only
- Interface: `DomainOperations` for DTO-based operations
- Conversion: Utilizes `DomainFacade.dtoConversion`
- Purpose: Anti-corruption layer between Application and Domain

#### Implementation Results
```haskell
-- Application.DTO.TodoDTO imports no domain modules
-- Application.DTO.Facade imports only DomainFacade
-- All DTO-Domain conversions occur within DomainFacade boundary
```

### Layer Responsibilities

- **Domain**: Value Objects, Entities, Events, and Aggregate logic (pure business rules)
  - Event sourcing implementation through `projectEvents` function
  - Domain validation through smart constructors
  - Business invariant enforcement
- **Application**: Use Cases, DTOs, Port interfaces, and orchestration
  - Service composition through `TodoService`
  - DTO-based external communication
  - Port definitions for infrastructure dependencies
- **Infrastructure**: Technical adapters implementing application ports
  - STM-based in-memory event store
  - Servant-based HTTP API with CORS support
  - JSON serialization and request/response handling

## File Structure

### Domain Layer
- `src/Domain/Todo/ValueObject.hs` - TodoId, TodoText value objects with smart constructors
- `src/Domain/Todo/Entity.hs` - Todo entity definition
- `src/Domain/Todo/Events.hs` - Domain events (Created, Completed, Uncompleted, Deleted)
- `src/Domain/Todo/Aggregate.hs` - Aggregate root with domain logic and consistency boundary

### Application Layer
- `src/Application/DTO/TodoDTO.hs` - Data Transfer Objects with JSON serialization
- `src/Application/DTO/Facade.hs` - Bridge adapter (`DomainOperations`) between layers
- `src/Application/TodoService.hs` - Use case implementations and service composition
- `src/Application/Ports/EventStorePort.hs` - Event storage interface
- `src/Application/Ports/NotificationPort.hs` - Event notification interface

### Infrastructure Layer
- `src/Infrastructure/EventStore/InMemory.hs` - STM-based event storage with `AppM`/`AppIO` monads
- `src/Infrastructure/Web/Types.hs` - HTTP request/response types and Servant API definition
- `src/Infrastructure/Web/Server.hs` - HTTP server implementation with CORS support

### Application Entry
- `app/Main.hs` - Application startup, dependency injection, sample data initialization

## Build and Execution

```bash
cabal build           # Build the project
cabal run todo-ddd-exe  # Run the server
# API endpoint: http://localhost:8080/api/todos
```

### API Endpoints

- `GET /api/todos`: List todos with statistics
- `POST /api/todos`: Create todo
- `POST /api/todos/toggle`: Toggle completion status
- `POST /api/todos/delete`: Delete todo
- `GET /api/events`: Event history
