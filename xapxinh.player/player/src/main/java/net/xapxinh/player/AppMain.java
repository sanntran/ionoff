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

import static net.xapxinh.player.Application.application;

import javax.swing.SwingUtilities;

import com.alee.laf.WebLookAndFeel;

import net.xapxinh.player.config.AppConfig;
import net.xapxinh.player.thread.InformStatusThread;
import net.xapxinh.player.thread.LatestVersionDownloader;

/**
 * Application entry-point.
 */
public class AppMain {

	public static void main(String[] args) throws InterruptedException {
		long heapSize = Runtime.getRuntime().totalMemory();
		System.out.println("Heap Size = " + heapSize);
		WebLookAndFeel.install();
		SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				new LatestVersionDownloader().start();
				if (AppConfig.getInstance().INTERVAL_UPDATE) {
					new InformStatusThread().start();
				}
				application().start();
			}
		});
	}
}
