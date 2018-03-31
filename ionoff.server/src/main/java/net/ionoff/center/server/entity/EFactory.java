package net.ionoff.center.server.entity;


public class EFactory {

	public static UserGroup newGroup(String groupName) {
		UserGroup group = new UserGroup();
		group.setName(groupName);
		return group;
	}
}