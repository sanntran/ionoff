package net.ionoff.center.server.notify;

public interface INotificationService {

	void sendSms(String[] subscribers, String message);

	void sendEmail(String[] subscribers, String message);

}
