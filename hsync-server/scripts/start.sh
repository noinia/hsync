#!/bin/sh

hsync-server Production | logger -p daemon.info -t hsync-server
