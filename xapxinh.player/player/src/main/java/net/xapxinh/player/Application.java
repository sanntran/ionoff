/*
 * This file is part of VLCJ.
 *
 * VLCJ is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * VLCJ is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with VLCJ.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2015 Caprica Software Limited.
 */

package net.xapxinh.player;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.ResourceBundle;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import com.google.common.eventbus.EventBus;

import net.xapxinh.player.connection.TcpConnection;
import net.xapxinh.player.event.ShutdownEvent;
import net.xapxinh.player.event.TickEvent;
import net.xapxinh.player.event.VolumeChangedEvent;
import net.xapxinh.player.systemtray.PlayerSystemTray;
import net.xapxinh.player.thread.PlayerSchedulesExecutor;
import net.xapxinh.player.view.action.mediaplayer.MediaPlayerActions;
import net.xapxinh.player.view.effects.EffectsFrame;
import net.xapxinh.player.view.main.MainFrame;
import net.xapxinh.player.view.messages.NativeLogFrame;
import uk.co.caprica.vlcj.log.NativeLog;
import uk.co.caprica.vlcj.player.embedded.EmbeddedMediaPlayer;
import uk.co.caprica.vlcj.runtime.RuntimeUtil;
import uk.co.caprica.vlcj.runtime.streams.NativeStreams;

/**
 * Global application state.
 */
public final class Application extends Vlcj {
	
	private static final NativeStreams nativeStreams;

	// Redirect the native output streams to files, useful since VLC can
	// generate a lot of noisy native logs we don't care about
	// (on the other hand, if we don't look at the logs we might won't see
	// errors)
	static {
		if (RuntimeUtil.isNix()) {
			nativeStreams = new NativeStreams("stdout.log", "stderr.log");
		} else {
			nativeStreams = null;
		}
	} 
	
	private JFrame mainFrame;

	@SuppressWarnings("unused")
	private JFrame messagesFrame;

	@SuppressWarnings("unused")
	private JFrame effectsFrame;

	private NativeLog nativeLog;

	private TcpConnection tcpConnection;
	
	private PlayerSystemTray systemTray;

    private static final String RESOURCE_BUNDLE_BASE_NAME = "strings/vlcj-player";

    private static final ResourceBundle resourceBundle = ResourceBundle.getBundle(RESOURCE_BUNDLE_BASE_NAME);

    private static final int MAX_RECENT_MEDIA_SIZE = 10;

    private final EventBus eventBus;

    private final EmbeddedMediaPlayerPanel mediaPlayerPanel;

    private final MediaPlayerActions mediaPlayerActions;

    private final ScheduledExecutorService tickService = Executors.newSingleThreadScheduledExecutor();

    private final Deque<String> recentMedia = new ArrayDeque<>(MAX_RECENT_MEDIA_SIZE);

    private static final class ApplicationHolder {
        private static final Application INSTANCE = new Application();
    }

    public static Application application() {
        return ApplicationHolder.INSTANCE;
    }

    public static ResourceBundle resources() {
        return resourceBundle;
    }

    @SuppressWarnings("serial")
	private Application() {
        eventBus = new EventBus();
        mediaPlayerPanel = new EmbeddedMediaPlayerPanel() {
            @Override
            protected String[] onGetMediaPlayerFactoryExtraArgs() {
                return new String[] {"--no-osd"}; // Disables the display of the snapshot filename (amongst other things)
            }
        };
       
        mediaPlayerActions = new MediaPlayerActions(mediaPlayerPanel.getMediaPlayer());
        tickService.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                eventBus.post(TickEvent.instance());
            }
        }, 0, 1000, TimeUnit.MILLISECONDS);
    }

    public void subscribe(Object subscriber) {
        eventBus.register(subscriber);
    }

    public void post(final Object event) {
        // Events are always posted and processed on the Swing Event Dispatch thread
        if (SwingUtilities.isEventDispatchThread()) {
            eventBus.post(event);
        }
        else {
            SwingUtilities.invokeLater(new Runnable() {
                @Override
                public void run() {
                    eventBus.post(event);
                }
            });
        }
    }

    public EmbeddedMediaPlayerPanel mediaPlayerPanel() {
        return mediaPlayerPanel;
    }

    public MediaPlayerActions mediaPlayerActions() {
        return mediaPlayerActions;
    }

    public void addRecentMedia(String mrl) {
        if (!recentMedia.contains(mrl)) {
            recentMedia.addFirst(mrl);
            while (recentMedia.size() > MAX_RECENT_MEDIA_SIZE) {
                recentMedia.pollLast();
            }
        }
    }

    public List<String> recentMedia() {
        return new ArrayList<>(recentMedia);
    }

    public void clearRecentMedia() {
        recentMedia.clear();
    }

	void start() {
		initFrame();
		tcpConnection = new TcpConnection();
		systemTray = new PlayerSystemTray(tcpConnection);
		mainFrame.setVisible(true);
		systemTray.start();
		tcpConnection.start();
		
		AppProperties.loadProperties();
		PlayerSchedulesExecutor.getInstance().start();
		
		int volume = AppProperties.getVolume();
		if (volume != -1) {
			mediaPlayerPanel().getMediaPlayer().setVolume(volume);
			application().post(VolumeChangedEvent.instance());
		}
		else {
			mediaPlayerPanel().getMediaPlayer().setVolume(75);
			application().post(VolumeChangedEvent.instance());
			AppProperties.setVolume(mediaPlayerPanel().getMediaPlayer().getVolume());
		}
	}

	private void initFrame() {
		mainFrame = new MainFrame();
		mainFrame.addWindowListener(new WindowAdapter() {
			@Override
			public void windowClosing(WindowEvent e) {
				quit();
			}
			@Override
			public void windowClosed(WindowEvent e) {
				// does nothing
			}  
		});
		mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		EmbeddedMediaPlayer embeddedMediaPlayer = mediaPlayerPanel
				.getMediaPlayer();
		embeddedMediaPlayer.setFullScreenStrategy(new FullScreenStrategy(
				mainFrame));

		nativeLog = mediaPlayerPanel.getMediaPlayerFactory().newLog();

		messagesFrame = new NativeLogFrame(nativeLog);
		effectsFrame = new EffectsFrame();
	}

	public void quit() {
		PlayerSchedulesExecutor.getInstance().quit();
		AppProperties.setVolume(mediaPlayerPanel.getMediaPlayer().getVolume());
		AppProperties.writeProperties();
		mediaPlayerPanel.getMediaPlayer().stop();
		mediaPlayerPanel.release();
		if (nativeStreams != null) {
			nativeStreams.release();
		}
		tcpConnection.close();
		application().post(ShutdownEvent.instance());
		System.exit(0);
	}

	public void playFiles(File[] files) {
		for (File file : files) {
			String mrl = file.getAbsolutePath();
			application().addRecentMedia(mrl);
		}
		mediaPlayerPanel.inEnqueue(files, true);
	}
}
