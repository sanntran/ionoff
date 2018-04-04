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

package net.xapxinh.player.event;

import java.awt.image.BufferedImage;

public final class SnapshotImageEvent {

    private BufferedImage image;

    private static SnapshotImageEvent instance;
    
    public static SnapshotImageEvent instance(BufferedImage image) {
    	if (instance == null) {
    		instance = new SnapshotImageEvent();
    	}
    	instance.setBufferedImage(image);
    	return instance;
    }
    
    private void setBufferedImage(BufferedImage image) {
    	this.image = image;
    }

    public BufferedImage getBufferedImage() {
        return image;
    }
}
