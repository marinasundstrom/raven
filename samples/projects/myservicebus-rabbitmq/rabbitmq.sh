#!/usr/bin/env bash

set -euo pipefail

container_name="raven-myservicebus-rabbitmq"
volume_name="raven-myservicebus-rabbitmq-data"
image_name="rabbitmq:3-management"

usage() {
  echo "Usage: ./rabbitmq.sh {start|stop|status|logs}"
}

require_docker() {
  if ! command -v docker >/dev/null 2>&1; then
    echo "Docker is required but was not found on PATH." >&2
    exit 1
  fi
}

repair_volume_permissions() {
  docker volume create "$volume_name" >/dev/null
  docker run --rm \
    --user root \
    --entrypoint sh \
    --volume "$volume_name:/var/lib/rabbitmq" \
    "$image_name" \
    -c 'chown -R rabbitmq:rabbitmq /var/lib/rabbitmq'
}

start_broker() {
  if docker inspect "$container_name" >/dev/null 2>&1; then
    if [[ "$(docker inspect --format '{{.State.Running}}' "$container_name")" != "true" ]]; then
      repair_volume_permissions
      docker start "$container_name" >/dev/null
    fi
  else
    repair_volume_permissions
    docker run --detach \
      --name "$container_name" \
      --hostname raven-rabbitmq \
      --publish 5672:5672 \
      --publish 15672:15672 \
      --env RABBITMQ_DEFAULT_USER=guest \
      --env RABBITMQ_DEFAULT_PASS=guest \
      --volume "$volume_name:/var/lib/rabbitmq" \
      "$image_name" >/dev/null
  fi

  echo "Waiting for RabbitMQ to accept connections..."
  for _ in {1..30}; do
    if docker exec "$container_name" rabbitmq-diagnostics -q ping >/dev/null 2>&1; then
      echo "RabbitMQ is ready."
      echo "AMQP:       amqp://guest:guest@localhost:5672"
      echo "Management: http://localhost:15672 (guest / guest)"
      return
    fi
    sleep 1
  done

  echo "RabbitMQ did not become ready within 30 seconds." >&2
  docker logs --tail 40 "$container_name" >&2
  exit 1
}

stop_broker() {
  if docker inspect "$container_name" >/dev/null 2>&1; then
    docker stop "$container_name" >/dev/null
    echo "Stopped $container_name. Its data volume was preserved."
  else
    echo "$container_name does not exist."
  fi
}

show_status() {
  if docker inspect "$container_name" >/dev/null 2>&1; then
    docker inspect --format '{{.Name}}: {{.State.Status}}' "$container_name"
  else
    echo "$container_name does not exist."
  fi
}

show_logs() {
  docker logs --follow "$container_name"
}

require_docker

case "${1:-}" in
  start) start_broker ;;
  stop) stop_broker ;;
  status) show_status ;;
  logs) show_logs ;;
  *)
    usage
    exit 2
    ;;
esac
