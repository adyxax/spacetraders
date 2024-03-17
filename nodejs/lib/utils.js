export function systemFromWaypoint(waypoint) {
	return waypoint.split('-').slice(0,2).join('-');
}
