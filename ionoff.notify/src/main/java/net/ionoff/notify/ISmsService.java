package net.ionoff.notify;

import java.io.IOException;

public interface ISmsService {
	String sendSms(String[] subcribers, String message) throws IOException;
}
