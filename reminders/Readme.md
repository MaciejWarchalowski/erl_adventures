# The event server has these tasks:

 * Accept subscriptions from clients
 * Forward notifications from event processes to each of the subscribers
 * Accept messages to add events (and start the x, y, and z processes needed)
 * Accept messages to cancel an event and subsequently kill the event processes

The event server can be terminated by a client, and it can have its code reloaded via the shell.

# The client has these tasks:

  * Subscribe to the event server and receive notifications as messages
  * Ask the server to add an event with all its details
  * Ask the server to cancel an event
  * Monitor the server (to know if it goes down)
  * Shut down the event server if needed

# The x, y z processes have the following tasks:
 * Send a message to the event server when the time is up
 * Receive a cancellation message and die

They represent a notification waiting to fire (they’re basically just timers linked to the event server).